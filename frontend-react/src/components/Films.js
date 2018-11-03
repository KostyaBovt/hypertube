import React, {Component} from 'react';
import { inject, observer } from 'mobx-react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import Paper from '@material-ui/core/Paper';


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

@inject('FilmsStore') @observer
class Films extends Component {

	render() {
		const { classes } = this.props;
       return (
       <Grid container className={classes.root} spacing={16}>
        <Grid item xs={12}>
          <Grid container className={classes.demo} justify="center" spacing={Number(8)}>
            {[0, 1, 2, 3].map(value => (
              <Grid key={value} item>
                <Paper className={classes.paper} />
              </Grid>
            ))}
          </Grid>
        </Grid>
      </Grid>
      );
    }
}

Films.propTypes = {
  classes: PropTypes.object.isRequired,
};

export default withStyles(styles)(Films);
