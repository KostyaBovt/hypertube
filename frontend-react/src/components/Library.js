import React, {Component} from 'react';
import { inject, observer } from 'mobx-react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import Paper from '@material-ui/core/Paper';

const styles = theme => ({
	
});


@inject('LibraryStore') @observer
class Library extends Component {
		constructor(props) {
		super(props);
		this.state = {
			isLoading: false,
			isDialogOpen: false,
		}
	}
    componentDidMount() {
        this.props.LibraryStore.fetchMovies();
    }

    render() {
        const { classes, LibraryStore } = this.props;


        return (
            <main className={classes.layout}>
                <Grid container spacing={8}>
                {LibraryStore.movies ?
                LibraryStore.movies.map(f => 
                	(<Grid key={f.id} item xs={9}>
                        <Paper>
                         {f.title}
                        </Paper>
                        <img src={f.poster_path}/>
                    </Grid>)
                ) : ""}
                    
                </Grid>
            </main>
        );
    }
}

Library.propTypes = {
  classes: PropTypes.object.isRequired,
};

export default withStyles(styles)(Library);