import React, {Component} from 'react';
import { inject, observer } from 'mobx-react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import Paper from '@material-ui/core/Paper';
import { Link } from 'react-router-dom'


const styles = theme => ({
  	root: {
    	flexGrow: 1,
  	},
  	paper: {
    height: 200,
    width: 150,
    margin: 20
  },
  control: {
    padding: theme.spacing.unit * 2,
  },
});

@inject('LibraryStore') @observer
class Home extends Component {

	render() {
		const { classes } = this.props;
       return (
        <Grid
          container
          direction="row"
          justify="center"
          alignItems="center"
          className={classes.root}
          spacing={16}
        >
          <Grid item xs={12}>
            <Grid container className={classes.demo} justify="center" spacing={Number(8)}>
              {[0, 1, 2, 3].map(value => (
                <Grid key={value} item>
                  <Link to={"/film/" + 'id'}><Paper className={classes.paper} /></Link>
                </Grid>
              ))}
            </Grid>
          </Grid>
        </Grid>
      );
    }
}

Home.propTypes = {
  classes: PropTypes.object.isRequired,
};

export default withStyles(styles)(Home);